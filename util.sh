HOST=localhost
PORT=8081
SERVER="$HOST:$PORT"

todo_tasks() {
  curl "$SERVER/tasks"
}

todo_create() {
  curl "$SERVER/tasks" -X POST -H "Content-Type: application/json" \
    -d '{"status":"Open","description":"'"$1"'"}'
}
