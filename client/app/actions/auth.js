import axios from 'axios'

import * as types from './types'
import { checkStatus } from '../utils/request'


/* Action Creators for Socket API authentication */
const SOCKET_API_URL = process.env.SOCKET_API_URL || 'http://localhost:5000'

export const connectSocket = (url, authToken) => ({
  type: types.CONNECT_SOCKET,
  url,
  authToken
})

export const disconnectSocket = () => ({ type: types.DISCONNECT_SOCKET })

export const logoutUser = () => dispatch => {
  localStorage.removeItem("token");
  dispatch(logout());
  dispatch(disconnectSocket());
}


/* Action Creators for User API authentication */
const AUTH_API_URL = process.env.AUTH_API_URL || 'http://localhost:8000'

export const authRequested = () => ({ type: types.AUTH_REQUESTED })

export const authSuccess = username => ({ type: types.AUTHENTICATED, username })

export const authError = error => ({ type: types.AUTHENTICATION_ERROR, error })

export const logout = () => ({ type: types.UNAUTHENTICATED })

export function login({ username, password }, history) {
  return async dispatch => {
    dispatch(authRequested())
    axios.post(`${AUTH_API_URL}/login`, {
      loginUsername: username,
      loginPassword: password
    }).then(({ data: { token } }) => {
      dispatch(authSuccess(username))
      dispatch(connectSocket(SOCKET_API_URL, token));
      localStorage.setItem('token', token)
      history.push('/lobby')
    }).catch(err => dispatch(authError(err)))
  }
}

export function register({ email, username, password }, history) {
  return async dispatch => {
    dispatch(authRequested())
    axios.post(`${AUTH_API_URL}/register`, {
      newUsername: username,
      newUserEmail: email,
      newUserPassword: password
    }).then(({ data: { token } }) => {
      dispatch(authSuccess(username))
      dispatch(connectSocket(SOCKET_API_URL, token));
      localStorage.setItem('token', token)
      history.push('/lobby')
    }).catch(err => dispatch(authError(err)))
  }
}
