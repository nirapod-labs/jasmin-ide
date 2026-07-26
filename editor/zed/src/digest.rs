//! SHA-256, so that the extension can check a downloaded server itself.
//!
//! The extension API this crate is built against (0.7.0, the newest that
//! stable Zed accepts) does not report the digest of a release asset; that
//! field arrives in 0.8.0, which stable Zed does not load. The digest of each
//! released server is therefore compiled into the extension, and the file is
//! checked here after the download and before it is made executable.
//!
//! A short implementation with no dependencies keeps the build free of new
//! crates and lets the check run in host tests as well as in the WebAssembly
//! build, where the same bytes must produce the same digest.

/// Round constants: the first 32 bits of the fractional parts of the cube
/// roots of the first 64 prime numbers (FIPS 180-4, section 4.2.2).
const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

/// The SHA-256 of `data`, as 32 bytes in the order the standard prints them.
pub fn sha256(data: &[u8]) -> [u8; 32] {
    let mut state = [
        0x6a09e667u32,
        0xbb67ae85,
        0x3c6ef372,
        0xa54ff53a,
        0x510e527f,
        0x9b05688c,
        0x1f83d9ab,
        0x5be0cd19,
    ];

    let mut blocks = data.chunks_exact(64);
    for block in blocks.by_ref() {
        compress(&mut state, block);
    }

    // The last block carries the padding and the message length in bits. It is
    // one block when at least eight bytes remain for the length, two otherwise.
    let remainder = blocks.remainder();
    let mut tail = [0u8; 128];
    tail[..remainder.len()].copy_from_slice(remainder);
    tail[remainder.len()] = 0x80;
    let tail_length = if remainder.len() < 56 { 64 } else { 128 };
    let bit_length = (data.len() as u64).wrapping_mul(8);
    tail[tail_length - 8..tail_length].copy_from_slice(&bit_length.to_be_bytes());
    for block in tail[..tail_length].chunks_exact(64) {
        compress(&mut state, block);
    }

    let mut digest = [0u8; 32];
    for (word, chunk) in state.iter().zip(digest.chunks_exact_mut(4)) {
        chunk.copy_from_slice(&word.to_be_bytes());
    }
    digest
}

/// Compresses one 64-byte block into the running state: sixty-four rounds over
/// the message schedule, per FIPS 180-4 section 6.2.2.
fn compress(state: &mut [u32; 8], block: &[u8]) {
    let mut schedule = [0u32; 64];
    for (word, chunk) in schedule.iter_mut().zip(block.chunks_exact(4)) {
        *word = u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
    }
    for index in 16..64 {
        let fifteen = schedule[index - 15];
        let two = schedule[index - 2];
        let small_sigma_zero = fifteen.rotate_right(7) ^ fifteen.rotate_right(18) ^ (fifteen >> 3);
        let small_sigma_one = two.rotate_right(17) ^ two.rotate_right(19) ^ (two >> 10);
        schedule[index] = schedule[index - 16]
            .wrapping_add(small_sigma_zero)
            .wrapping_add(schedule[index - 7])
            .wrapping_add(small_sigma_one);
    }

    let mut working = *state;
    for index in 0..64 {
        let [a, b, c, d, e, f, g, h] = working;
        let big_sigma_one = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
        let choose = (e & f) ^ ((!e) & g);
        let first = h
            .wrapping_add(big_sigma_one)
            .wrapping_add(choose)
            .wrapping_add(K[index])
            .wrapping_add(schedule[index]);
        let big_sigma_zero = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
        let majority = (a & b) ^ (a & c) ^ (b & c);
        let second = big_sigma_zero.wrapping_add(majority);
        working = [
            first.wrapping_add(second),
            a,
            b,
            c,
            d.wrapping_add(first),
            e,
            f,
            g,
        ];
    }

    for (word, value) in state.iter_mut().zip(working) {
        *word = word.wrapping_add(value);
    }
}

/// A digest as lowercase hexadecimal, the usual spelling in release notes.
pub fn to_hex(digest: &[u8]) -> String {
    let mut hex = String::with_capacity(digest.len() * 2);
    for byte in digest {
        hex.push(char::from_digit((byte >> 4) as u32, 16).unwrap_or('0'));
        hex.push(char::from_digit((byte & 0x0f) as u32, 16).unwrap_or('0'));
    }
    hex
}

/// Checks the SHA-256 of the file at `path` against `expected_hex`.
///
/// A file that does not match is deleted before the error is returned, so that
/// a truncated or substituted download can never be made executable and run.
/// The error names the file and both digests.
pub fn verify_file(path: &str, expected_hex: &str) -> Result<(), String> {
    let contents = std::fs::read(path)
        .map_err(|error| format!("could not read {path} to check its SHA-256: {error}"))?;
    let actual = to_hex(&sha256(&contents));
    if actual.eq_ignore_ascii_case(expected_hex) {
        return Ok(());
    }

    let deletion = match std::fs::remove_file(path) {
        Ok(()) => "the file was deleted".to_string(),
        Err(error) => format!("the file could not be deleted either: {error}"),
    };
    Err(format!(
        "the SHA-256 of {path} is {actual}, not the expected {expected_hex}; {deletion}"
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// FIPS 180-4 and the usual published vectors, one per message length
    /// class: empty, one block, exactly the padding boundary, and many blocks.
    #[test]
    fn published_vectors() {
        assert_eq!(
            to_hex(&sha256(b"")),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert_eq!(
            to_hex(&sha256(b"abc")),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        assert_eq!(
            to_hex(&sha256(
                b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
            )),
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        );
        assert_eq!(
            to_hex(&sha256(&[b'a'; 1_000_000])),
            "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
        );
    }

    #[test]
    fn a_matching_digest_is_accepted_and_the_file_kept() {
        let path = scratch_file("matching.bin", b"abc");
        assert!(verify_file(
            &path,
            "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"
        )
        .is_ok());
        assert!(std::fs::metadata(&path).is_ok());
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn a_mismatched_digest_is_refused_and_the_file_deleted() {
        let path = scratch_file("mismatched.bin", b"a server that is not the released one");
        let error = verify_file(
            &path,
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        )
        .expect_err("a mismatched digest must be refused");

        assert!(error.contains(&path), "the error names the file: {error}");
        assert!(error.contains("the file was deleted"), "{error}");
        assert!(
            std::fs::metadata(&path).is_err(),
            "the refused file must be gone"
        );
    }

    /// Writes `contents` to a fresh file under the system temporary directory
    /// and returns its path, so that the tests write nothing into the source
    /// tree and do not collide with each other.
    fn scratch_file(name: &str, contents: &[u8]) -> String {
        let directory =
            std::env::temp_dir().join(format!("jasmin-ide-digest-{}", std::process::id()));
        std::fs::create_dir_all(&directory).expect("a temporary directory for the digest test");
        let path = directory.join(name);
        std::fs::write(&path, contents).expect("the test file");
        path.to_str().expect("a UTF-8 path").to_string()
    }
}
