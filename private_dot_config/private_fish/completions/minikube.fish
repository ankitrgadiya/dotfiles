-*- shell-script -*-
# Copyright 2016 The Kubernetes Authors.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# fish completion for minikube

function __minikube_debug
    set file "$BASH_COMP_DEBUG_FILE"
    if test -n "$file"
        echo "$argv" >> $file
    end
end

function __minikube_perform_completion
    __minikube_debug "Starting __minikube_perform_completion with: $argv"

    set args (string split -- " " "$argv")
    set lastArg "$args[-1]"

    __minikube_debug "args: $args"
    __minikube_debug "last arg: $lastArg"

    set emptyArg ""
    if test -z "$lastArg"
        __minikube_debug "Setting emptyArg"
        set emptyArg \"\"
    end
    __minikube_debug "emptyArg: $emptyArg"

    set requestComp "$args[1] __complete $args[2..-1] $emptyArg"
    __minikube_debug "Calling $requestComp"

    set results (eval $requestComp 2> /dev/null)
    set comps $results[1..-2]
    set directiveLine $results[-1]

    # For Fish, when completing a flag with an = (e.g., <program> -n=<TAB>)
    # completions must be prefixed with the flag
    set flagPrefix (string match -r -- '-.*=' "$lastArg")

    __minikube_debug "Comps: $comps"
    __minikube_debug "DirectiveLine: $directiveLine"
    __minikube_debug "flagPrefix: $flagPrefix"

    for comp in $comps
        printf "%s%s\n" "$flagPrefix" "$comp"
    end

    printf "%s\n" "$directiveLine"
end

# This function does three things:
# 1- Obtain the completions and store them in the global __minikube_comp_results
# 2- Set the __minikube_comp_do_file_comp flag if file completion should be
#    performed and unset it otherwise
# 3- Return true if the completion results are not empty
function __minikube_prepare_completions
    # Start fresh
    set --erase __minikube_comp_do_file_comp
    set --erase __minikube_comp_results

    # Check if the command-line is already provided.  This is useful for testing.
    if not set --query __minikube_comp_commandLine
        set __minikube_comp_commandLine (commandline)
    end
    __minikube_debug "commandLine is: $__minikube_comp_commandLine"

    set results (__minikube_perform_completion "$__minikube_comp_commandLine")
    set --erase __minikube_comp_commandLine
    __minikube_debug "Completion results: $results"

    if test -z "$results"
        __minikube_debug "No completion, probably due to a failure"
        # Might as well do file completion, in case it helps
        set --global __minikube_comp_do_file_comp 1
        return 0
    end

    set directive (string sub --start 2 $results[-1])
    set --global __minikube_comp_results $results[1..-2]

    __minikube_debug "Completions are: $__minikube_comp_results"
    __minikube_debug "Directive is: $directive"

    if test -z "$directive"
        set directive 0
    end

    set compErr (math (math --scale 0 $directive / 1) % 2)
    if test $compErr -eq 1
        __minikube_debug "Received error directive: aborting."
        # Might as well do file completion, in case it helps
        set --global __minikube_comp_do_file_comp 1
        return 0
    end

    set nospace (math (math --scale 0 $directive / 2) % 2)
    set nofiles (math (math --scale 0 $directive / 4) % 2)

    __minikube_debug "nospace: $nospace, nofiles: $nofiles"

    # Important not to quote the variable for count to work
    set numComps (count $__minikube_comp_results)
    __minikube_debug "numComps: $numComps"

    if test $numComps -eq 1; and test $nospace -ne 0
        # To support the "nospace" directive we trick the shell by outputting an
        # extra, longer completion.
        __minikube_debug "Adding second completion to perform nospace directive"
        set --append __minikube_comp_results $__minikube_comp_results[1].
    end

    if test $numComps -eq 0; and test $nofiles -eq 0
        __minikube_debug "Requesting file completion"
        set --global __minikube_comp_do_file_comp 1
    end

    # If we don't want file completion, we must return true even if there are no
    # completions found. This is because fish will perform the last completion
    # command, even if its condition is false, if no other completion command
    # was triggered
    return (not set --query __minikube_comp_do_file_comp)
end

# Remove any pre-existing completions for the program since we will be handling
# all of them TODO this cleanup is not sufficient. Fish completions are only
# loaded once the user triggers them, so the below deletion will not work as it
# is run too early. What else can we do?
complete -c minikube -e

# The order in which the below two lines are defined is very important so that
# __minikube_prepare_completions is called first. It is
# __minikube_prepare_completions that sets up the __minikube_comp_do_file_comp
# variable.
#
# This completion will be run second as complete commands are added FILO. It
# triggers file completion choices when __minikube_comp_do_file_comp is set.
complete -c minikube -n 'set --query __minikube_comp_do_file_comp'

# This completion will be run first as complete commands are added FILO. The
# call to __minikube_prepare_completions will setup both __minikube_comp_results
# abd __minikube_comp_do_file_comp. It provides the program's completion
# choices.
complete -c minikube -n '__minikube_prepare_completions' -f -a '$__minikube_comp_results'
