import { readFileSync } from "node:fs";

// A failed assertion has to fail the run. `assert` used to print FAIL and the
// process exited 0, so `spago test` said "Test succeeded" over a failure
// (found 2026-11-04). Counted here, and the exit code set as the process ends.
let failures = 0;
process.on("exit", () => {
  if (failures > 0) {
    console.log(failures + " assertion(s) FAILED");
    process.exitCode = 1;
  }
});

export const recordFailureImpl = () => { failures++; };

export const readTextImpl = (path) => () => readFileSync(path, "utf8");
