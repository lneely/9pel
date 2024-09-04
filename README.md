# Emacs 9P Server

This project implements a 9P server in Emacs Lisp. The 9P protocol is
a network protocol used for distributed file systems, originally
developed for the Plan 9 operating system.

## Features

- Server can be started, stopped, and restarted from within Emacs
- Work in progress: Implementation of the 9P2000 protocol
- Work in progress: Virtual filesystem for RPC functionality
- Work in progress: vfs and 9p server integration.

## 9P2000 Protocol (Work in Progress)

The implementation of the 9P2000 protocol is currently under development.

### Usage

```
;; start the 9p server
(9p-start-server)

;; stop the 9p server
(9p-stop-server)

;; restart a running 9p server
(9p-restart-server)
```

```
# send ls command using plan9port 9p client
9p -a 'unix!/tmp/emacs-9p-server.sock' ls /
```

## Virtual Filesystem (Work in Progress)

An RPC interface will be exposed over 9P to enable remote interactions
with Emacs. The implementation is currently under development.

### Usage

```
;; a functioning example
(vfs-register-directory "/b/")
(vfs-register-fileop "/b/" 'directory-files 'vfs-op-b-directory-files)

;; returns a list of open buffers
(directory-files "/b/")
```

### Limitations

- Currently only supports registering a fileops handler for individual
  directories; the handler registration does not propagate to
  subdirectories.

## Testing

9pel uses my custom testing framework, [LispyMcTestFace](https://github.com/lneely/lmtf) 
for unit and integration testing. Give it a try, you might like it.
