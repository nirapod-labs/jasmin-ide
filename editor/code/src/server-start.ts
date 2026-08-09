/**
 * The launch plan: the executable, the arguments, the transport and the
 * `initialize` options the client hands to `LanguageClient`.
 *
 * The module imports nothing, for two reasons. It keeps the plan readable on
 * its own, and it lets a plain Node test spawn the same executable with the
 * same arguments the extension spawns inside VS Code, where `extension.ts`
 * cannot be imported because it needs the editor's API.
 */

/** The settings that shape how the server is started. */
export interface LaunchSettings {
  readonly serverArgs: readonly string[];
  readonly diagnosticsDelay: number;
  readonly arch: string;
  readonly include: Readonly<Record<string, string>>;
}

/** The `initialize` options the client sends, under the names the server reads. */
export interface InitializationOptions {
  readonly diagnosticsDelay: number;
  readonly arch: string;
  readonly include: Record<string, string>;
}

/** The one transport the server offers: the protocol on standard input and output. */
export type LaunchTransport = "stdio";

/** Everything the client needs to start the server and complete `initialize`. */
export interface ServerLaunch {
  readonly command: string;
  readonly args: readonly string[];
  readonly transport: LaunchTransport;
  readonly initializationOptions: InitializationOptions;
}

/**
 * Builds the plan from the resolved executable and the settings.
 *
 * The arguments are the `server.args` setting and nothing else: the server
 * takes no argument to serve, so an installation that leaves the setting at
 * its default is started with an empty argument list.
 */
export function planServerLaunch(command: string, settings: LaunchSettings): ServerLaunch {
  return {
    command,
    args: [...settings.serverArgs],
    transport: "stdio",
    initializationOptions: {
      diagnosticsDelay: settings.diagnosticsDelay,
      arch: settings.arch,
      include: {...settings.include}
    }
  };
}
