// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package main

import (
	"fmt"
	"os"
	"strings"

	"path"

	"io/ioutil"

	"github.com/chzyer/readline"
	"github.com/tbogdala/gimmick"
)

var (
	flags cmdFlags
)

const (
	cmdUnknown = iota
	cmdREPL
	cmdVersion
)

const (
	versionString  = `VERSION 0.1.0`
	welcomeMessage = `GIMMICK REPL. COPYRIGHT 2017 BY TIMOTHY BOGDALA <TDB@ANIMAL-MACHINE.COM>.
THIS IS OPEN SOURCE SOFTWARE UNDER THE BSD LICENSE.
` + versionString + `
`
)

type cmdFlags struct {
	exeName     string
	command     int
	commandArgs []string
}

// printUsage prints the command-line usage of the program
func printUsage() {
	fmt.Println("usage: gimmick <command>")
	fmt.Println("")
	fmt.Println("commands: ")
	fmt.Println("  repl <file.gmk>*")
	fmt.Println("  version")
	fmt.Println("")
}

// parseArgs parses the command-line arguments
func parseArgs(args []string) {
	// must at least consist of the executable and a command
	argLen := len(args)
	if argLen < 2 {
		fmt.Printf("Not enough command-line arguments provided (%d).\n\n", argLen)
		printUsage()
		os.Exit(1)
	}

	// parse  executable name
	flags.exeName = args[0]

	// parse out the command
	switch args[1] {
	case "repl":
		flags.command = cmdREPL
	case "version":
		flags.command = cmdVersion
	default:
		flags.command = cmdUnknown
	}

	// set the remainder of the args, if any
	if len(args) > 2 {
		flags.commandArgs = args[2:]
	}
}

func loadFile(filename string, env *gimmick.Environment) error {
	fmt.Printf("Loading file %s ...\n", filename)
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to load file %s: %v", filename, err)
	}

	// load the code into the environment after removing the CR
	// from the source file string.
	fileCode := strings.Replace(string(bytes), "\r", "", -1)
	fileSexp, err := gimmick.ParseString(fileCode)
	if err != nil {
		return fmt.Errorf("failed to parse the file %s: %v", filename, err)
	}

	gimmick.Eval(fileSexp, env)
	return nil
}

func createReplEnvironment() (*gimmick.Environment, error) {
	// setup a new environment with
	replEnv := gimmick.NewEnvironment(nil)
	replEnv.SetupPrimitives()

	// if there were files specified as command arguments on the
	// command-line, load them now.
	for _, cmdArg := range flags.commandArgs {
		if _, err := os.Stat(cmdArg); !os.IsNotExist(err) {
			if path.Ext(cmdArg) == ".gmk" {
				err := loadFile(cmdArg, replEnv)
				if err != nil {
					return replEnv, err
				}
			}
		}
	}

	fmt.Printf("\n")
	return replEnv, nil
}

// runRepl runs the read-eval-print-loop
func runRepl() error {
	const basicPrompt = "> "
	const multilinePrompt = ">> "

	// start up the readline library
	rl, err := readline.New(basicPrompt)
	if err != nil {
		return fmt.Errorf("Unable to start the readline library: %v", err)
	}
	defer rl.Close()

	fmt.Printf(welcomeMessage)

	// create the gimmick environment
	replEnv, err := createReplEnvironment()
	if err != nil {
		return err
	}

	// do the REPL loop
	var codeString string
	var lastCodeString string
	var lastResult gimmick.Value = gimmick.List{}
	for {
		line, err := rl.Readline()
		if err != nil {
			return fmt.Errorf("Error while reading input: %v", err)
		}

		// handle some non-gimmick REPL commands
		trimmedLine := strings.TrimSpace(line)
		switch trimmedLine {
		case ".quit":
			// .quit command exits the repl
			return nil
		case ".print":
			// .print displays the current sexp being entered
			fmt.Printf("Current sexp: %s\n\n", codeString)
			continue
		case ".reset":
			// .reset command resets the NewEnvironment
			replEnv = gimmick.NewEnvironment(nil)
			replEnv.SetupPrimitives()
			continue
		case ".parse":
			// .parse shows the parse message for the current sexp
			_, err := gimmick.ParseString(lastCodeString)
			if err == nil {
				fmt.Printf("Sexp parses without error.\n\n")
			} else {
				fmt.Printf("Sexp parse error: %v\n\n", err)
			}
			continue
		case ".type":
			// .type shows the type of the last result
			fmt.Printf("Type of last result is: %s\n\n", gimmick.GetTypeString(lastResult))
			continue
		case "":
			// empty line erases current command
			codeString = ""
			rl.SetPrompt(basicPrompt)
			continue
		}

		// try to parse the code provided such far as an expression
		combinedCode := codeString + " " + line
		value, err := gimmick.ParseString(combinedCode)

		// if we got an error, append this line to the accumulator
		// and keep looping with the multiline prompt.
		if err != nil {
			codeString = combinedCode
			rl.SetPrompt(multilinePrompt)
		} else {
			// it parsed, so evalute the sexp
			lastResult = gimmick.Eval(value, replEnv)
			if _, isLastProcedure := lastResult.(gimmick.Procedure); isLastProcedure {
				fmt.Printf("= <procedure>\n\n")
			} else {
				fmt.Printf("= %v\n\n", lastResult)
			}
			rl.SetPrompt(basicPrompt)
			lastCodeString = combinedCode
			codeString = ""
		}
	}

}

func main() {
	// parse the command line arguments
	parseArgs(os.Args)

	// branch out to the command
	var err error
	switch flags.command {
	case cmdREPL:
		err = runRepl()
	case cmdVersion:
		fmt.Printf(versionString)
	}

	// handle any error conditions that are returned from the commands
	if err != nil {
		fmt.Printf("An error was encountered: %v\n", err)
		os.Exit(1)
	}
}
