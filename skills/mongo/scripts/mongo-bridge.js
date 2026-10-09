// (agent:run "You have access to mongo with `(uiop:run-program \"mongosh --quiet --eval ...\" :output :string)`")

const net = require('net');

const LOCAL_PORT = 27017;
const LOCAL_HOST = '127.0.0.1';
const REMOTE_PORT = 27017;
// host-gateway, set up by run.sh's --add-host -- lisp-agent's own
// $NETWORK has no fixed address for the host the way the default
// "bridge" network does at 172.17.0.1.
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
		console.error(`Upstream ${REMOTE_HOST}:${REMOTE_PORT} error: ${err.message}`);
		socket.end();
	});
});

server.listen(LOCAL_PORT, LOCAL_HOST, () => {
	console.log(`Bridge listening on ${LOCAL_HOST}:${LOCAL_PORT} -> ${REMOTE_HOST}:${REMOTE_PORT}`);
});
