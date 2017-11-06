// Copyright 2017, Timothy Bogdala <tdb@animal-machine.com>
// See the LICENSE file for more details.

package gimmick

import (
	"context"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/labstack/echo"
)

// RuseCommand aliases the int type for commands to send to the Ruse server
type RuseCommand int

const (
	ruseCommandQuit = 10
)

// startRuse starts the Ruse http server to listen for commands via the web api.
func startRuse(sexp Value, env *Environment) Value {
	// sexp should be a list: (start-ruse hostname :string, port :int)
	// the default options are hostname="localhost" and port=4005
	list, haveList := sexp.(List)
	if !haveList || len(list) < 1 {
		return Bool(false)
	}

	hostname := String("localhost")
	portnum := Integer(4005)

	if len(list) >= 2 {
		s, hasHostname := list[1].(String)
		if !hasHostname {
			return Bool(false)
		}
		hostname = s
	}

	if len(list) >= 3 {
		p, hasPort := list[2].(Integer)
		if !hasPort {
			return Bool(false)
		}
		portnum = p
	}

	// if we already have a ruse server return false
	if env.ruseCmd != nil {
		return Bool(false)
	}

	// setup the command channel and routes for the Ruse server
	env.ruseCmd = make(chan RuseCommand)
	e := buildRoutes(env)

	// gracefully stop the server on termination signal
	stop := make(chan os.Signal)
	signal.Notify(stop, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-stop
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()
		if err := e.Shutdown(ctx); err != nil {
			log.Printf("Error while stopping Ruse server: %v\n", err)
		}

		// pass the message on the quit channel that the server was stopped
		env.ruseCmd <- ruseCommandQuit
	}()

	// create the HTTP server
	go func() {
		e.HideBanner = true
		e.Logger.SetOutput(ioutil.Discard) // Disables Echo's built-in logging.
		if err := e.Start(fmt.Sprintf("%s:%d", hostname, portnum)); err != nil {
			log.Printf("Ruse server shutting down.")
		}
	}()

	// create a listener for ruse commands
	go func() {
		select {
		case cmd := <-env.ruseCmd:
			if cmd == ruseCommandQuit {
				stop <- os.Interrupt
				return
			}
		}
	}()

	return Bool(true)
}

// stopRuse stops any running Ruse server for the environment.
func stopRuse(sexp Value, env *Environment) Value {
	// sexp should be (stop-ruse)

	// if there's no command channel then there's nothing to stop
	if env.ruseCmd == nil {
		return Bool(false)
	}

	env.ruseCmd <- ruseCommandQuit
	return Bool(true)
}

func buildRoutes(env *Environment) *echo.Echo {
	e := echo.New()
	e.POST("/ruse/eval", ruseHandleEval(env))
	return e
}

// ruseHandleEval evaluates a block of code
func ruseHandleEval(env *Environment) echo.HandlerFunc {
	return func(c echo.Context) error {
		reqBody, err := ioutil.ReadAll(c.Request().Body)
		if err != nil {
			return c.String(http.StatusBadRequest, "Failed to read the request body: "+err.Error())
		}

		value, err := ParseString(string(reqBody))
		if err != nil {
			return c.String(http.StatusBadRequest, "Failed to parse the code: "+err.Error())
		}

		result := Eval(value, env)
		if _, isLastProcedure := result.(Procedure); isLastProcedure {
			return c.String(http.StatusOK, "<procedure>")
		}

		return c.String(http.StatusOK, fmt.Sprintf("%v", result))
	}
}
