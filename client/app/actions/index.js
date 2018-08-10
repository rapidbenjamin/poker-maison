import axios from 'axios';

import * from './types'

const URL = 'http://www.sample-website.com';

export function signInAction({ loginEmail, loginPassword }, history) {
  return async (dispatch) => {
    try {
      const res = await axios.post(`${URL}/signin`, {
        loginEmail, loginPassword
      });

      dispatch({ type: AUTHENTICATED, username });
      localStorage.setItem('user', res.data.token);
      history.push('/lobby');
    } catch (error) {
      dispatch({
        type: AUTHENTICATION_ERROR,
        payload: 'Invalid email or password'
      });
    }
  };
}

export function signupAction({ newUserEmail, newUserUsername, newUserPassword }, history) {
  return async (dispatch) => {
    try {
      const res = await axios.post(`${URL}/signin`, {
        newUserUsername,
        newUserEmail,
        newUserPassword
      });

      dispatch({ type: AUTHENTICATED, username });
      localStorage.setItem('user', res.data.token);
      history.push('/lobby');
    } catch (error) {
      dispatch({
        type: AUTHENTICATION_ERROR,
        payload: 'Invalid email or password'
      });
    }
  };
}
