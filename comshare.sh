#!/bin/bash
# comshare - enhanced environment sharing

# Default configuration
PORT=9999
TIMEOUT=5

# Error handling
set -e
trap 'echo "Error: Command failed at line $LINENO"' ERR

# Helper functions
show_usage() {
    cat << EOF
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
EOF
}

check_server() {
    nc -z localhost $PORT >/dev/null 2>&1
    return $?
}

start_server() {
    if ! check_server; then
        echo "Starting server on port $PORT..."
        ./share &
        sleep 1  # Give server time to start
        if check_server; then
            echo "✓ Server started"
        else
            echo "✗ Server failed to start"
            exit 1
        fi
    else
        echo "Server already running on port $PORT"
    fi
}

stop_server() {
    if check_server; then
        pkill -f "./share"
        echo "✓ Server stopped"
    else
        echo "No server running"
    fi
}

share_() {
    if ! check_server; then
        echo "✗ Server not running"
        exit 1
    fi
    echo "Sharing environment..."
    if timeout $TIMEOUT com | nc localhost $PORT; then
        echo "✓ Environment shared"
    else
        echo "✗ Share failed"
        exit 1
    fi
}

get_com() {
    if ! check_server; then
        echo "✗ Server not running"
        exit 1
    fi
    echo "Getting environment..."
    if OUTPUT=$(timeout $TIMEOUT nc localhost $PORT); then
        if [ -n "$OUTPUT" ]; then
            echo "$OUTPUT" | source /dev/stdin
            echo "✓ Environment received and applied"
        else
            echo "✗ No environment available"
            exit 1
        fi
    else
        echo "✗ Get failed"
        exit 1
    fi
}

# Parse arguments
COMMAND=""
while [[ $# -gt 0 ]]; do
    case $1 in
        share|get|start|stop)
            COMMAND="$1"
            shift
            ;;
        -p|--port)
            PORT="$2"
            shift 2
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Execute command
case "$COMMAND" in
    "share")
        share_com
        ;;
    "get")
        get_com
        ;;
    "start")
        start_server
        ;;
    "stop")
        stop_server
        ;;
    *)
        show_usage
        exit 1
        ;;
esac
