import axios from 'axios';

import * as types from './types';
import { UNAUTHENTICATED } from './types';

const AUTH_API_URL = process.env.AUTH_API_URL || 'http://localhost:8000'

export function signIn({ loginEmail, loginPassword }, history) {
  return async dispatch => {
    try {
      const res = await axios.post(`${AUTH_API_URL}/signin`, {
        loginEmail, loginPassword
      });

      dispatch(authSuccess(loginEmail)); // TODO should be username
      localStorage.setItem('user', res.data.token);
      history.push('/lobby');
    } catch (error) {
      dispatch(authError(error));
    }
  };
}

export function signup({ newUserEmail, newUserUsername, newUserPassword }, history) {
  return async dispatch => {
    try {
      const res = await axios.post(`${AUTH_API_URL}/signin`, {
        newUserUsername,
        newUserEmail,
        newUserPassword
      });

      dispatch(authSuccess(newUserUsername));
      localStorage.setItem('user', res.data.token);
      history.push('/lobby');
    } catch (error) {
      dispatch(authError(error))
    }
  };
}

export const authSuccess = username => ({ type: types.AUTHENTICATED, username })

export const authError = error => ({ type: types.AUTHENTICATION_ERROR, error })

export const logout = () => ({ type: types.UNAUTHENTICATED })