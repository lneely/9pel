# Emacs 9P Server

This project implements a 9P server in Emacs Lisp. The 9P protocol is
a network protocol used for distributed file systems, originally
developed for the Plan 9 operating system.

## Features

- Server can be started, stopped, and restarted from within Emacs
- Supports reloading of server code without stopping the server
- WIP: Implementation of the 9P2000 protocol

## Key Functions

- `9p-start-server`: Starts the 9P server
- `9p-stop-server`: Stops the 9P server
- `9p-restart-server`: Restarts the 9P server

## Key Bindings

9p.el currently sets `C-c 9` to `9p-restart-server` for developer 
convenience.

