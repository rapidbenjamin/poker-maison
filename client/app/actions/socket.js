import * as types from './types'

export const socketConnErr = err => ({ type: types.SOCKET_CONN_ERR, err })

export const socketConnected = socket => ({ type: types.SOCKET_CONNECTED, socket })

export const socketAuthSuccess = () => ({ type: types.SOCKET_AUTH_SUCCESS })

export const socketAuthErr = err => ({ type: types.SOCKET_AUTH_ERR, err })

export const socketReconnecting = () => ({ type: "SOCKET_RECONNECTING" })

export const socketReconnectFail = () => ({ type: "SOCKET_RECONNECT_FAIL" })