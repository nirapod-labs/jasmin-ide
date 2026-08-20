/** The part of an `initialize` result the version check reads. */
export interface InitializeResultLike {
  readonly serverInfo?: {
    readonly version?: string;
  };
}

/**
 * Compares the extension's version with the version the server reports in
 * `serverInfo`.
 *
 * Returns the one warning to show, or undefined when there is nothing to
 * report: the versions match, this extension has no version, or the server
 * sends no `serverInfo` or no version in it. A server that reports no version
 * is not an error, because the field is optional in the protocol.
 */
export function versionMismatchMessage(
  clientVersion: string | undefined,
  initializeResult: InitializeResultLike | undefined
): string | undefined {
  const serverVersion = initializeResult?.serverInfo?.version;
  if (serverVersion === undefined || serverVersion === "") {
    return undefined;
  }
  if (clientVersion === undefined || clientVersion === serverVersion) {
    return undefined;
  }

  return (
    `The Jasmin language server reports version ${serverVersion}, ` +
    `but this extension is version ${clientVersion}. ` +
    "Some features may be missing or may not work."
  );
}
