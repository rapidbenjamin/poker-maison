import { fromJS } from 'immutable'

import { disconnectSocket } from '../actions/auth'
import {
  socketConnErr,
  socketConnected,
  socketAuthErr,
  socketAuthSuccess
} from '../actions/socket'
import { newLobby } from '../actions/lobby'
import { newGameState } from '../actions/games'

import * as types from '../actions/types'

const SOCKET_API_URL =
  process.env.NODE_ENV === 'production'
    ? process.env.SOCKET_API_URL || 'ws://18.130.171.128:5000'
    : process.env.SOCKET_API_URL || 'ws://localhost:5000'

function addHandlers(socket, authToken, dispatch) {
  socket.onopen = event => {
    // connected to server but not authenticated
    dispatch(socketConnected(socket)) // pass ref to socket so dispatcher - socket middleware has access to new connected socket instance
    socket.send(authToken)
  }

  socket.onclose = event => {
    dispatch(disconnectSocket())
  }

  socket.onmessage = msg => {
    console.log(msg)

    const parsedMsg = JSON.parse(JSON.parse(msg.data))
    if (parsedMsg.tag === 'AuthSuccess') {
      dispatch(socketAuthSuccess())
    }
    if (parsedMsg.tag === 'TableList') {
      const tableList = parsedMsg.contents
      dispatch(newLobby(fromJS(tableList)))
    }
    if (
      parsedMsg.tag === 'SuccessfullySatDown' ||
      parsedMsg.tag === 'NewGameState' ||
      parsedMsg.tag === 'SuccessfullySubscribedToTable'
    ) {
      const tableName = parsedMsg.contents[0]
      const gameState = parsedMsg.contents[1]
      dispatch(newGameState(tableName, fromJS(gameState)))
    }
  }

  socket.onerror = err => {
    console.log(err)
    dispatch(socketAuthErr(err))
    console.log(err)
  }
}

let connectedSocket = null

function connHandler(dispatch, action) {
  if (action.type === types.CONNECT_SOCKET) {
    const { token } = action
    connectedSocket = new WebSocket(SOCKET_API_URL)
    addHandlers(connectedSocket, token, dispatch)
  }

  if (action.type === types.DISCONNECT_SOCKET && connectedSocket) {
    if (connectedSocket.readyState === 1) {
      connectedSocket.close()
    }
  }

  if (action.data && connectedSocket) {
    if (connectedSocket.readyState === 1)
      connectedSocket.send('data', action.payload)
  }
}

/**
 * Allows you to register actions that when dispatched, send the action to the
 * server via a socket.
 * `criteria` may be a function (type, action) that returns true if you wish to send the
 *  action to the server, array of action types, or a string prefix.
 * the third parameter is an options object with the following properties:
 * {
 *   eventName,// a string name to use to send and receive actions from the server.
 *   execute, // a function (action, emit, next, dispatch) that is responsible for
 *            // sending the message to the server.
 * }
 */
const reduxSocketMiddleware = ({ dispatch, getState }) => next => action => {
  connHandler(dispatch, action)
  console.log(action)
  const criteria = 'server/'

  if (connectedSocket) {
    if (evaluate(action, criteria)) {
      return defaultExecute(dispatch, next, action)
    }
  }
  return next(action)
}

function evaluate(action, option) {
  if (!action || !action.type) {
    return false
  }

  const { type } = action
  let matched = false
  if (typeof option === 'function') {
    // Test function
    matched = option(type, action)
  } else if (typeof option === 'string') {
    // String prefix
    matched = type.indexOf(option) === 0
  } else if (Array.isArray(option)) {
    // Array of types
    matched = option.some(item => type.indexOf(item) === 0)
  }
  return matched
}

function defaultExecute(dispatch, next, action) {
  if (connectedSocket) {
    if (connectedSocket.readyState === 1)
      connectedSocket.send(JSON.stringify(action.data))
  }
}

export default reduxSocketMiddleware
