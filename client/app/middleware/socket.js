/**
* Look to move this into its own npm modules
* 
*/

// two authenticatation steps getting jwt and storing in local storage
// then using the jwt to authentiate socket handshake using jwt-socket-io middleware
export function socketAuthSuccess() {
  return {
    type: "SOCKET_AUTH_SUCCESS"
  };
}

export function socketAuthErr(err) {
  return {
    type: "SOCKET_AUTH_ERR",
    err
  };
}

export function socketReconnecting() {
  return {
    type: "SOCKET_RECONNECTING"
  };
}

export function socketReconnectFail() {
  return {
    type: "SOCKET_RECONNECT_FAIL"
  };
}

export function socketConnected(socket) {
  return {
    type: "SOCKET_CONNECTED",
    socket
  };
}

export function socketConnectErr(err) {
  return {
    type: "SOCKET_CONNECT_ERR",
    err
  };
}

export function socketDisconnect() {
  return {
    type: "SOCKET_DISCONNECT"
  };
}

function addHandlers(socket, authToken, dispatch, eventName) {
  socket.on("connect", () => {
    // connected to server but not authenticated
    dispatch(socketConnected(socket)); // pass ref to socket so dispatcher - socket middleware has access to new connected socket instance
    socket.emit("data", authToken);
  });

  socket.on("authenticated", () => {
    dispatch(socketAuthSuccess());
  });

  socket.on("unauthorized", err => {
    dispatch(socketAuthErr(err));
  });

  socket.on("connect_error", err => {
    dispatch(socketConnectErr(err));
  });

  socket.on("connect_timeout", err => {
    dispatch(socketConnectErr(err));
  });

  // Fired upon an attempt to reconnect.
  socket.on("reconnecting", attemptNumber => {
    dispatch(socketReconnecting());
  });

  // Fired upon a reconnection attempt error.
  socket.on("reconnect_error", err => {
    //  dispatch(socketReconnectFail(err))
  });

  // Fired when couldn’t reconnect within reconnectionAttempts
  socket.on("reconnect_failed", err => {
    // dispatch(socketReconnectFail(err))
  });

  socket.on("disconnect", () => {
    dispatch(socketDisconnect());
  });

  // when socket receives any event with given name then dispatch the action payload to store
  connectedSocket.on(eventName, dispatch);
}

let connectedSocket = null;

function connHandler(dispatch, action, socket, eventName) {
  if (action.type === "CONNECT_SOCKET") {
    connectedSocket = socket(action.url);
    addHandlers(connectedSocket, action.authToken, dispatch, eventName);
  }

  if (action.type === "DISCONNECT_SOCKET") {
    if (connectedSocket) {
      connectedSocket.disconnect();
    }
  }
}

/**
* Allows you to register actions that when dispatched, send the action to the
* server via a socket.io socket.
* `criteria` may be a function (type, action) that returns true if you wish to send the
*  action to the server, array of action types, or a string prefix.
* the third parameter is an options object with the following properties:
* {
*   eventName,// a string name to use to send and receive actions from the server.
*   execute, // a function (action, emit, next, dispatch) that is responsible for
*            // sending the message to the server.
* }
*/
const reduxSocketIo = (socket, criteria = [], eventName = "data") => ({
  dispatch,
  getState
}) => next => action => {
  connHandler(dispatch, action, socket, eventName);
  console.log(action);

  if (connectedSocket) {
    if (evaluate(action, criteria)) {
      return defaultExecute(next, connectedSocket, eventName, action);
    }
    return next(action);
  }
  return next(action);

};

function evaluate(action, option) {
  if (!action || !action.type) {
    return false;
  }

  const { type } = action;
  let matched = false;
  if (typeof option === "function") {
    // Test function
    matched = option(type, action);
  } else if (typeof option === "string") {
    // String prefix
    matched = type.indexOf(option) === 0;
  } else if (Array.isArray(option)) {
    // Array of types
    matched = option.some(item => type.indexOf(item) === 0);
  }
  return matched;
}

function defaultExecute(next, connectedSocket, eventName, action) {
  connectedSocket.emit(eventName, action);
  return next(action);
}

export default reduxSocketIo;