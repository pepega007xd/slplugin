import benchexec
import benchexec.result as result


class Tool(benchexec.tools.template.BaseTool2):
    def executable(self, tool_locator):
        return tool_locator.find_executable("frama-c")

    def version(self, executable):
        return "0.1"

    def name(self):
        return "slplugin"

    def project_url(self):
        return "https://github.com/pepega007xd/slplugin"

    def cmdline(self, executable, options, task, rlimits):
        args = "-ulevel=3 -scf -sl -sl-benchmark-mode -sl-no-catch-exceptions -sl-astral-encoding Bitvectors -sl-backend-solver Bitwuzla -sl-edge-deduplication -sl-simple-join"
        input_file = task.input_files[0]
        input_file = input_file[:-1] + "c"  # use .c file instead of .i file
        return [executable] + args.split() + options + [input_file]

    def determine_result(self, run):
        if run.exit_code.value != 0:
            return result.RESULT_ERROR

        if run.output.any_line_contains("Invalid_deref"):
            return result.RESULT_FALSE_DEREF

        if run.output.any_line_contains("Invalid_free"):
            return result.RESULT_FALSE_FREE

        if run.output.any_line_contains("leak of atom"):
            return result.RESULT_FALSE_MEMTRACK

        return result.RESULT_TRUE_PROP
