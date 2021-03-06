// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

import (
	"bufio"
	"bytes"
	"fmt"
	"strconv"
	"strings"
)

// ParseString takes a source code chunk in string form and parses it returning
// the resulting Value or an error.
func ParseString(source string) (Value, error) {
	tokens := buildTokens(source)
	sexp, _, err := buildValueFromTokens(tokens)
	return sexp, err
}

// buildTokens parses the source string into tokens.
func buildTokens(source string) []token {
	sr := strings.NewReader(source)
	buffString := bufio.NewReader(sr)
	tokens := tokenize(buffString, "")
	return tokens
}

// buildValueFromTokens parses the incoming tokens and returns the resulting
// value. An error is returned on parsing failures.
func buildValueFromTokens(tokens []token) (Value, []token, error) {
	if tokens == nil || len(tokens) < 1 {
		return nil, nil, fmt.Errorf("unexpected end of source code")
	}

	t := tokens[0]
	remainingTokens := tokens[1:]

	switch t.Type {
	case tokenOPENPARENS, tokenQUASIQUOTE:
		// create the new list
		sexp := List{}
		// loop through tokens until we get a close parens
		for len(remainingTokens) != 0 && remainingTokens[0].Type != tokenCLOSEPARENS {
			newVal, remaining, err := buildValueFromTokens(remainingTokens)
			if err != nil {
				return sexp, remaining, err
			}
			if newVal != nil {
				sexp = sexp.appendValue(newVal)
			}
			remainingTokens = remaining
		}

		// if this was a quasiquote, then what we do is put all of this
		// in a separate list that starts with the symbol `quasiquote`.
		var finalSexp List
		if t.Type == tokenQUASIQUOTE {
			finalSexp = List{Symbol("quasiquote"), sexp}
		} else {
			// no change if this was a non-quasiquote parens
			finalSexp = sexp
		}

		// go one more token forward in the remaining list to remove the close parens,
		// but only if there are tokens after it
		if len(remainingTokens) > 1 {
			return finalSexp, remainingTokens[1:], nil
		}
		// if there are no tokens left, that means we hit the end without a close parens,
		// so issue a parse error
		if len(remainingTokens) == 0 {
			return finalSexp, nil, NewSyntaxError("Missing ')' to close sexp.", t.Filename, t.LineNumber)
		}

		// theres just 1 token left and it was a close parens, so just return the sexp
		return finalSexp, nil, nil

	case tokenCLOSEPARENS:
		// close parens are handled in the loop for open parens, so extra ones are errors
		return nil, tokens, NewSyntaxError("Unexpected ')' found.", t.Filename, t.LineNumber)

	case tokenSYMBOL:
		return Symbol(t.Token), remainingTokens, nil

	case tokenINTEGER:
		iVal, err := strconv.ParseInt(t.Token, 10, 64)
		if err != nil {
			return nil, tokens, NewSyntaxError(fmt.Sprintf("Unable to convert value (%s) to integer", t.Token), t.Filename, t.LineNumber)
		}
		return Integer(iVal), remainingTokens, nil

	case tokenFLOAT:
		fVal, err := strconv.ParseFloat(t.Token, 64)
		if err != nil {
			return nil, tokens, NewSyntaxError(fmt.Sprintf("Unable to convert value (%s) to float", t.Token), t.Filename, t.LineNumber)
		}
		return Float(fVal), remainingTokens, nil

	case tokenBOOL:
		bVal, err := strconv.ParseBool(t.Token)
		if err != nil {
			return nil, tokens, NewSyntaxError(fmt.Sprintf("Unable to convert value (%s) to bool", t.Token), t.Filename, t.LineNumber)
		}
		return Bool(bVal), remainingTokens, nil

	case tokenSTRING:
		return String(t.Token), remainingTokens, nil

	case tokenUNQUOTE:
		if len(remainingTokens) == 0 {
			return nil, remainingTokens, NewSyntaxError("Unexpected \",\" found with no more remaining tokens.", t.Filename, t.LineNumber)
		}
		uqList := List{Symbol("unquote")}

		// take the next value
		newVal, remainingTokens, err := buildValueFromTokens(remainingTokens)
		if err != nil {
			return nil, remainingTokens, err
		}

		// add it to the unquote list and return that.
		uqList = append(uqList, newVal)
		return uqList, remainingTokens, nil

	case tokenUNQUOTESPLICING:
		if len(remainingTokens) == 0 {
			return nil, remainingTokens, NewSyntaxError("Unexpected \",@\" found with no more remaining tokens.", t.Filename, t.LineNumber)
		}
		uqList := List{Symbol("unquote-splicing")}

		// take the next value
		newVal, remainingTokens, err := buildValueFromTokens(remainingTokens)
		if err != nil {
			return nil, remainingTokens, err
		}

		// add it to the unquote list and return that.
		uqList = append(uqList, newVal)
		return uqList, remainingTokens, nil

	case tokenILLEGAL:
		return nil, remainingTokens, NewSyntaxError(fmt.Sprintf("Illegal token found while parsing (%s).", t.Token), t.Filename, t.LineNumber)

	default:
		return nil, tokens, NewSyntaxError(fmt.Sprintf("Parsed token is not handled while building expression (token:\"%s\" type:%d).", t.Token, t.Type), t.Filename, t.LineNumber)
	}
}

func (l List) appendValue(v Value) List {
	return append(l, v)
}

// read in a rune from the stream and return rune(0) on EOF or error.
func read(source *bufio.Reader) rune {
	// read in the next rune
	ch, _, err := source.ReadRune()
	if err != nil {
		return rune(0)
	}
	return ch
}

func unread(source *bufio.Reader) {
	source.UnreadRune()
}

// lexical Analysis
func tokenize(source *bufio.Reader, filename string) []token {
	// this is all of the tokens to return
	tokens := []token{}

	currentLine := 1
	stringMode := false
	var stringBuffer bytes.Buffer
	for {
		// read in the next rune
		ch := read(source)
		if ch == rune(0) { // EOF
			return tokens
		}

		// deal with string mode first. if a string is being tokenized, everything
		// goes into the string until the ending doublequote is found. this will
		// include newline characters.
		if ch == rune('"') {
			if !stringMode {
				// if we're not already capturing a string, lets start
				stringMode = true

			} else {
				// we've finished with the string, so add it as a token
				stringMode = false
				tokens = append(tokens, token{
					Token:      stringBuffer.String(),
					Type:       tokenSTRING,
					LineNumber: currentLine})
				stringBuffer.Reset()
			}
			continue
		} else {
			// it's not a double quote, so if we're in string mode just gobble all runes
			if stringMode {
				stringBuffer.WriteRune(ch)
				continue
			}
		}

		// parse the rune/token
		if isWhitespace(ch) {
			unread(source)
			currentLine = scanWhitespace(source, filename, currentLine)
		} else if ch == '(' {
			tokens = append(tokens, token{Token: "(", Type: tokenOPENPARENS, LineNumber: currentLine})
		} else if ch == ')' {
			tokens = append(tokens, token{Token: ")", Type: tokenCLOSEPARENS, LineNumber: currentLine})
		} else if ch == '`' {
			next := read(source)
			if next == '(' {
				tokens = append(tokens, token{Token: "`(", Type: tokenQUASIQUOTE, LineNumber: currentLine})
			} else {
				unread(source)
			}
		} else if ch == ',' {
			next := read(source)
			if next != '@' {
				unread(source)
				tokens = append(tokens, token{Token: ",", Type: tokenUNQUOTE, LineNumber: currentLine})
			} else {
				tokens = append(tokens, token{Token: ",@", Type: tokenUNQUOTESPLICING, LineNumber: currentLine})
			}
		} else if ch == ';' {
			gobbleRestOfLine(source) // comments go until the end of the line
			currentLine++
		} else {
			unread(source)
			tokens = append(tokens, scanAtom(source, filename, currentLine))
		}
	}
}

func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\n' || ch == '\t'
}

func isNewline(ch rune) bool {
	return ch == '\n'
}

func isParens(ch rune) bool {
	return ch == '(' || ch == ')'
}

func isNumber(ch rune) bool {
	return (ch >= '0' && ch <= '9')
}

// gobbleRestOfLines reads the rest of the runes until a newline appears.
func gobbleRestOfLine(source *bufio.Reader) {
	for {
		c := read(source)
		if c == rune(0) || isNewline(c) {
			break
		}
	}
}

// scanWhitespace reads from the source Reader until a non-whitespace character is
// found. The return value is the new line number to use in parsing, updated for
// all of the newline characters scanWhitespace ate.
func scanWhitespace(source *bufio.Reader, filename string, line int) int {
	// if we're in this function, then there's at least one whitespace rune
	firstChar := read(source)
	lineCountToAdd := 0
	if isNewline(firstChar) {
		lineCountToAdd++
	}

	// now keep reading runes until we find a non-whitespace one
	for {
		ch := read(source)
		if ch == rune(0) { // EOF
			break
		} else if !isWhitespace(ch) { // !WS
			unread(source)
			break
		} else {
			if isNewline(ch) { // lc++ on newline
				lineCountToAdd++
			}
		}
	}

	// add to the line cout for all of the newline characters found
	return line + lineCountToAdd
}

// scanAtom gets called for everything that's not whitespace or '(' or ')'
// and must therefore distinguish between the other types such as numbers,
// bools and strings.
func scanAtom(source *bufio.Reader, filename string, line int) token {
	var t token
	var b bytes.Buffer

	// now keep reading runes until we find a whitespace or parens
	for {
		ch := read(source)
		if ch == rune(0) {
			break
		} else if isWhitespace(ch) || isParens(ch) {
			unread(source)
			break
		} else {
			b.WriteRune(ch)
		}
	}

	// at this point, we have our 'word' string, so make it a token
	t.Token = b.String()
	t.Filename = filename
	t.LineNumber = line

	// check for bool symbols
	if t.Token == "true" {
		t.Type = tokenBOOL
		return t
	}
	if t.Token == "false" {
		t.Type = tokenBOOL
		return t
	}

	// try to read as an integer
	_, err := strconv.ParseInt(t.Token, 10, 64)
	if err == nil {
		t.Type = tokenINTEGER
		return t
	}

	// try to read as a float
	_, err = strconv.ParseFloat(t.Token, 64)
	if err == nil {
		t.Type = tokenFLOAT
		return t
	}

	// at this point we treat it as a symbol
	t.Type = tokenSYMBOL
	return t
}
