import axios from 'axios'

import * as types from './types'
import { checkStatus } from '../utils/request'

/* Action Creators for Socket API authentication */

// Redux Socket Middleware intercepts this action and handles connection logic
export const connectSocket = token => ({
  type: types.CONNECT_SOCKET,
  token
})

export const disconnectSocket = () => ({ type: types.DISCONNECT_SOCKET })

export const logoutUser = history => dispatch => {
  localStorage.removeItem('token')
  dispatch(logout())
  dispatch(disconnectSocket())
  history.push('/')
}

console.log('env var', process.env)

const AUTH_API_URL = process.env.AUTH_API_URL ? 'http://209.97.176.231' : 'http://localhost:8000'

export const authRequested = () => ({ type: types.AUTH_REQUESTED })

export const authSuccess = username => ({ type: types.AUTHENTICATED, username })

export const authError = error => ({ type: types.AUTHENTICATION_ERROR, error })

export const logout = () => ({ type: types.UNAUTHENTICATED })

export function login(username, password, history) {
  return async dispatch => {
    dispatch(authRequested())
    axios
      .post(`${AUTH_API_URL}/login`, {
        loginUsername: username,
        loginPassword: password
      })
      .then(({ data }) => {
        const { access_token } = data
        dispatch(authSuccess(username))
        dispatch(connectSocket(access_token))
        localStorage.setItem('token', JSON.stringify({ ...data, username }))
        history.push('/profile')
      })
      .catch(err => dispatch(authError(err)))
  }
}

export function register(username, email, password, history) {
  return async dispatch => {
    dispatch(authRequested())
    axios
      .post(`${AUTH_API_URL}/register`, {
        newUsername: username,
        newUserEmail: email,
        newUserPassword: password
      })
      .then(({ data }) => {
        const { access_token } = data
        dispatch(authSuccess(username))
        dispatch(connectSocket(access_token))
        localStorage.setItem('token', JSON.stringify({ ...data, username }))
        history.push('/profile')
      })
      .catch(err => dispatch(authError(err)))
  }
}
