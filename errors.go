// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

import (
	"fmt"
)

// SyntaxError is an error type for syntax problems in the source code.
type SyntaxError struct {
	// Message is the error message for the syntax error
	Message string

	// Filename is the name of the file associated with the error
	Filename string

	// LineNumber is the line number where the error occurred
	LineNumber int
}

// NewSyntaxError creates a new error value for syntax problems.
func NewSyntaxError(msg string, fn string, ln int) *SyntaxError {
	return &SyntaxError{Message: msg, Filename: fn, LineNumber: ln}
}

// Error prints the syntax error message out to a string.
func (err *SyntaxError) Error() string {
	if err.Filename != "" {
		return fmt.Sprintf("Syntax error: %s (%s:%d)", err.Message, err.Filename, err.LineNumber)
	}
	return fmt.Sprintf("Syntax error: %s (line: %d)", err.Message, err.LineNumber)

}
