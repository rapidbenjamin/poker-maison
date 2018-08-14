import axios from 'axios'

import * as types from './types'
import { checkStatus } from '../utils/request'



// signs us out and disconnects any socket conn
export function logoutUser() {
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

export function login({ loginUsername, loginPassword }, history) {
  return async dispatch => {
    try {
      dispatch(authRequested())
      const res = await axios.post(`${AUTH_API_URL}/login`, {
        loginUsername,
        loginPassword
      })

      checkStatus(res)
      dispatch(authSuccess(loginUsername))
      dispatch(connectSocket(SOCKET_API_URL, token));
      localStorage.setItem('token', res.data.token)
      history.push('/lobby')
    } catch (error) {
      dispatch(authError(error))
    }
  }
}

export function register({ newUserEmail, newUsername, newUserPassword }, history) {
  return async dispatch => {
    try {
      dispatch(authRequested())
      const res = await axios.post(`${AUTH_API_URL}/register`, {
        newUsername,
        newUserEmail,
        newUserPassword
      })

      checkStatus(res)
      dispatch(authSuccess(newUsername))
      dispatch(connectSocket(SOCKET_API_URL, token));
      localStorage.setItem('token', res.data.token)
      history.push('/lobby')
    } catch (error) {
      dispatch(authError(error))
    }
  }
}


/* Action Creators for Socket API authentication */
const SOCKET_API_URL = process.env.SOCKET_API_URL || 'http://localhost:5000'


export const connectSocket = (url, authToken) => ({
  type: types.CONNECT_SOCKET,
  url,
  authToken
})

export const disconnectSocket = () => ({ type: types.DISCONNECT_SOCKET })