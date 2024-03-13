# Server Protocol

This document presents the why3find server protocol used to build proof
clusters.  Recall that a proof cluster consists of: a unique _server_ instance
responsible for the coordination and management of the cluster; many (volatile)
_client_ instances polling proof requests to the _server_; many (volatile)
_worker_ instances actually running proof tasks. Both _clients_ and _workers_
connects the _server_ on the same URL address.

## Transport Sockets

All kind of instances are using [ZeroMQ](hhtps://zeromq.org) sockets to
communicate with each others. Both UNIX Sockets and TCP transport protocol can
be used for connecting. A typical cluster setup is composed as follows:

- The Server instance _binds_ to `tcp://*:5555` URL on some host named `HOST`;
- All Client and Worker instances _connect_ to `tcp://HOST:5555`.

## Server Database

The _Server_ instance stores the proof cluster knowledge in a local directory
named the _Server Database_. This directory is composed of the following files
and subdirectories:

- `profile.json` contains the prover calibration data (Cf. `why3find config
  -m`);
- `AGE/*` contains the proof cache entries for generation `AGE`, where `AGE=0`
  is the currently active generation and `AGE>0` are older ones.

Proof entries are automatically promoted from older generations to currently
active one. Archiving is performed by invoking `why3find server --prune N`,
where the `N` most recent generations are shifted to `1..N`, and older ones are
erased. Invoking `why3find server --stats` allows checking for disk usage in
each generation.

## Server Messages

Since both clients and workers are connecting to the same address on the Server,
they actually share the same protocol. However, workers and clients actually
use a different subset of the protocol messages, with few common commands.

The protocol is using _Dealer_ ZMQ Sockets on Client and Worker sides, and
_Router_ ZMQ Sockets on Server side. The protocol is then asynchronous in both
directions for all participants to the cluster.

### Messages Summary

Below is a summary of commands and responses of the protocol, presented
in a typical order of events:

| Name       | Usage            | Description                   |
|:-----------|:----------------:|-------------------------------|
| `PROFILE`  | `C <-> S <-> W`  | Prover calibration            |
| `GET`      | `C -> S`         | Proof request                 |
| `DOWNLOAD` | `C <- S`         | Proof data download request   |
| `UPLOAD`   | `C -> S`         | Proof data upload to server   |
| `RAISE`    | `S -> W`         | Workers needed                |
| `READY`    | `W -> S`         | Available worker              |
| `PROVE`    | `S -> W`         | Proof task request to worker  |
| `RESULT`   | `C <-> S <- W`   | Proof result transfer         |
| `KILL`     | `C -> S <-> W`   | Proof task cancelling         |
| `HANGUP`   | `C -> S <- W`    | Leaving participant           |

### Arguments Format

| Arg.   | Type      | Example          | Usage                 |
|:------:|:---------:|:-----------------|:----------------------|
| `prv`  | _string_  | `alt-ergo@2.2.0` | Prover description    |
| `jobs` | _int_     | `8`              | Available cores       |
| `size` | _int_     | `45`             | Calibration size      |
| `time` | _float_   | `0.55`           | Calibration time or timeout |
| `hash` | _string_  | `41c04a2df870…`  | Proof task digest     |
| `data` | _string_  | An SMTLIB formula | proof task data       |

### PROFILE

    Usage: Client <-> Server -> Worker
    Format: [ PROFILE | prv | size | time ]

    Usage: Worker -> Server
    Format: [ PROFILE | prv ]

Updates the target calibration with the given prover profile, if undefined yet.
Any participant to the cluster shall first announce to each others their
respective prover profile, if any. This is usually done this way: Client
announce its calibration profile to the Server, which replies back its own
profile. Worker does not need to calibrate themselves, they simply ask the
Server to send its profile by emitting a `PROFILE` message with no size nor
time.

Notice that Clients and Workers are both responsible for converting proof times
and timeouts with respect to Server calibration profile. The Server _never_
convert times on its own.

### GET

    Usage: Client -> Server
    Format: [ GET | prv | hash | timout ]

Proof request from Client. The expected response is eventually a `RESULT`,
although the server might first ask for a `DOWNLOAD` if the proof task data has
not been uploaded to the Server yet. The `timout` limit must be converted by the
Client with respect to the Server's profile.

### DOWNLOAD

    Usage: Server -> Client
    Format: [ DOWNLOAD | prv | hash ]

Request from the Server to the Client for uploading proof task data.
Usually sent in response to a `GET` command from the Client.

### UPLOAD

    Usage: Client -> Server
    Format: [ UPLOAD | prv | hash | data ]

Upload proof data from the Client to the Server.
Usually sent in response to a `DOWNLOAD` message from the Server.

### RAISE

    Usage: Server -> Worker
    Format: [ RAISE ]

Sent by Server to previously known Workers for update. This message makes the
Server able to keep track of silently disconnected workers after a period of
inactivity. Workers are invited to re-emit `READY` messages in response to the
`RAISE` message.

### READY

    Usage: Worker -> Server
    Format: [ READY | jobs | prv | … | prv ]

Announce some available cores for the listed provers. The Server will assign at
most `jobs` proof tasks to the originated worker, and only for the specified
provers.

### PROVE

    Usage: Server -> Worker
    Format: [ PROVE | prv | hash | timeout | data ]

Proof task assigned to a worker. The `timeout` must be converted by the Worker
with respect to the Server's profile.

### RESULT

    Usage: Worker -> Server <-> Client
    Format: [ RESULT | prv | hash | status | time ]

Sends a proof result, from either Client or Worker to Server, or from Server to
Client. Status can be either: `Valid`, `Invalid`, `Timeout` or `Unknown`.
Failures are discarded.

### KILL

    Usage: Client -> Server <-> Worker
    Format: [ KILL | prv | hash ]

Cancel a proof request, either on the Server or on a Worker. A pending proof
request in the Server shall be killed by all its waiting Clients before actually
being canceled in Worker(s).

### HANGUP

    Usage: Client -> Server <- Worker
    Format: [ HANGUP ]

Fairly announce that a Worker or Client is disconnecting. This actually
cancels the associated pending proof requests and triggers a re-scheduling
of the associated running proof tasks. Notice that Workers and Clients often
disconnect on-the-fly, typically after a user `Ctrl-C` in terminal.
