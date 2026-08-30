/**
 * The command ids the extension registers.
 *
 * `package.json` contributes the same ids; a test reads both so the manifest
 * and the registrations cannot drift apart.
 */
export const restartCommand = "jasminIde.restartServer";
export const showLogCommand = "jasminIde.showServerLog";
