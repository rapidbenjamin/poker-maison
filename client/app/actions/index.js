import axios from 'axios';

import * from './types';
import { UNAUTHENTICATED } from './types';

const AUTH_API_URL = Process.env.AUTH_API_URL || 'http://localhost:8000'

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

export const authSuccess = username => ({{ type: AUTHENTICATED, username })

export const authErr = error => ({ type: AUTHENTICATION_ERROR, error })

export const logout = () => ({ type: UNAUTHENTICATED })