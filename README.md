# Com Share

A minimal environment variable sharing tool written in Haskell.

## What it does

Shares environment variables between shells through a simple netcat pipe. When one shell sends its environment, the next connection receives it. By default data exists only in memory and is destroyed after first use or 60-second timeout.

## Usage

```bash
# Terminal 1 (share your com)
com | nc localhost 9999

# Terminal 2 (receive the com)
nc localhost 9999 | source
```

## Build

```bash
ghc -threaded share.hs
```

## Run server

```bash
./share.sh
```
## For shell scripts to make life even easier...Make executeable

```chmod +x comshare.sh```

## Share your com
```
Usage: com [command] [options]

Commands:
    share       Share current environment
    get         Get shared environment
    start      Start the Haskell server
    stop       Stop the server

Options:
    -p, --port PORT    Use specific port (default: 9999)
    -t, --timeout SEC  Connection timeout (default: 5)
    -h, --help        Show this help
```

## Dependencies

- GHC (Glasgow Haskell Compiler)
- network package
- If you have no idea how GHC combobulation works,
  ```sudo ./haskell_nix_build.sh``` and then follow the directions ```y``` & ```Enter``` through the steps and you should end up with an executable for the server. ```./share.sh``` when finished to run the server. 


## **Example**
### Developer Alice's working environment   
### Terminal 1
```
$ echo $NODE_ENV
```
in production
```
$ echo $PATH
/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games
$ echo $NODE_PATH
/usr/lib/node_modules
$ com | nc localhost 9999
```

#### Developer Bob's terminal with issues
#### Terminal 2
```
$ echo $NODE_ENV
development    # <-- Different!
$ echo $NODE_PATH
              # <-- Missing!
```
### Bob receives Alice's environment
```
$ nc localhost 9999 | source
```
### Bob's environment is now identical
```
$ echo $NODE_ENV
```
in production
```
$ echo $NODE_PATH
/usr/lib/node_modules
```
### Node.js app now works as expected
```
$ node app.js
Server started successfully!
```
##Contributing
If you would like to make a contribution to this project... Don't hesitate, message me. This place is full of snakes, and bugs and who knows what else lurking in the cod.e.                                                                                                                     🕷️
