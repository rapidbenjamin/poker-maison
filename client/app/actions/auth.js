import axios from 'axios'

import * as types from './types'
import { checkStatus } from '../utils/request'

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
      localStorage.setItem('token', res.data.token)
      history.push('/lobby')
    } catch (error) {
      dispatch(authError(error))
    }
  }
}
