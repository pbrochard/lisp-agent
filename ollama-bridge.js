// Relays 127.0.0.1:11434 inside the container to `ollama serve` on the
// host, so agent-ollama.lisp's hardcoded localhost endpoint keeps working
// unchanged. See skills/mongo/scripts/mongo-bridge.js for the same pattern.

const net = require('net');

const LOCAL_PORT = 11434;
const LOCAL_HOST = '127.0.0.1';
const REMOTE_PORT = 11434;
// host-gateway, set up by run.sh's --add-host.
const REMOTE_HOST = 'host.docker.internal';

const server = net.createServer((socket) => {
	const client = net.createConnection(REMOTE_PORT, REMOTE_HOST, () => {
		socket.pipe(client);
		client.pipe(socket);
	});

	socket.on('error', (err) => {
		client.end();
	});
	client.on('error', (err) => {
		socket.end();
	});
});

server.listen(LOCAL_PORT, LOCAL_HOST, () => {
	console.log(`Bridge listening on ${LOCAL_HOST}:${LOCAL_PORT} -> ${REMOTE_HOST}:${REMOTE_PORT}`);
});
