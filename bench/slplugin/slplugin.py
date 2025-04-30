import benchexec
import benchexec.result as result


class Tool(benchexec.tools.template.BaseTool2):
    def executable(self, tool_locator):
        return tool_locator.find_executable("sh")

    def version(self, executable):
        return "0.1"

    def name(self):
        return "slplugin"

    def project_url(self):
        return "https://github.com/pepega007xd/slplugin"

    def get_args(self, input_file, ulevel):
        return f"frama-c -scf -ulevel={ulevel} {
            input_file} -then-replace -sl -sl-benchmark-mode -sl-no-catch-exceptions -sl-astral-encoding Bitvectors -sl-backend-solver Bitwuzla -sl-edge-deduplication -sl-simple-join"

    def cmdline(self, executable, options, task, rlimits):
        input_file = task.input_files[0]
        input_file = input_file[:-1] + "c"  # use .c file instead of .i file

        args_ulevel_3 = self.get_args(input_file, 3)
        args_ulevel_2 = self.get_args(input_file, 2)
        args_ulevel_0 = self.get_args(input_file, 0)

        return [executable, "-c", f"timeout {rlimits.cputime // 3} {args_ulevel_3} || timeout {rlimits.cputime // 3} {args_ulevel_2} || {args_ulevel_0}"]

    def determine_result(self, run):
        if run.exit_code.value != 0:
            return result.RESULT_ERROR

        if run.output.any_line_contains("Invalid_deref"):
            return result.RESULT_FALSE_DEREF

        if run.output.any_line_contains("Invalid_free"):
            return result.RESULT_FALSE_FREE

        if run.output.any_line_contains("leak of atom"):
            return result.RESULT_FALSE_MEMTRACK

        if run.output.any_line_contains("unknown result"):
            return result.RESULT_UNKNOWN

        return result.RESULT_TRUE_PROP
