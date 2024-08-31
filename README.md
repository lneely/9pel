# Emacs 9P Server

This project implements a 9P server in Emacs Lisp. The 9P protocol is
a network protocol used for distributed file systems, originally
developed for the Plan 9 operating system.

## Features

- Server can be started, stopped, and restarted from within Emacs
- Supports reloading of server code without stopping the server
- Work in progress: Implementation of the 9P2000 protocol
- Work in progress: Virtual filesystem for RPC functionality
- Work in progress: vfs and 9p server integration.

## Key Functions

- `9p-start-server`: Starts the 9P server
- `9p-stop-server`: Stops the 9P server
- `9p-restart-server`: Restarts the 9P server
- `9p-reload-and-restart`: Reloads the current buffer and restarts the server

## 9P2000 Protocol Implementation (Work in Progress)

The implementation of the 9P2000 protocol is currently under development.

## Virtual Filesystem for RPC (Work in Progress)

An RPC interface will be exposed over 9P to enable remote interactions
with Emacs. The implementation is currently under development.