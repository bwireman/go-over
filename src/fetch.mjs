import fetch from 'sync-fetch';

export function do_fetch(path) {
    return fetch(path, { headers: { "User-Agent": "gleam.go-over.fetch" } }).text()
}
