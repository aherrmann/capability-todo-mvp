HOST=localhost
PORT=8081
SERVER="$HOST:$PORT"

todo_tasks() {
  curl -s "$SERVER/tasks" | jq
}

todo_create() {
  curl -s "$SERVER/tasks" -X POST -H "Content-Type: application/json" \
    -d '{"status":"Open","description":"'"$1"'"}'
}

todo_update() {
  curl -s "$SERVER/tasks/$1" -X PUT -H "Content-Type: application/json" \
    -d '{"status":"'"$2"'","description":"'"$3"'"}'
}

todo_delete() {
  curl -s "$SERVER/tasks/$1" -X DELETE
}
