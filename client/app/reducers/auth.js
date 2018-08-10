import Immutable from 'immutable';

import { AUTHENTICATED, UNAUTHENTICATED, AUTHENTICATION_ERROR } from '../actions/types';

const initialState = Immutable.Map({ authenticated: false, username: null });

export default function (state = initialState, action) {
  switch (action.type) {
    case AUTHENTICATED:
      return state.set('authenticated', true).set('username', action.username);
    case UNAUTHENTICATED:
      return state.set('authenticated', false).set('username', null);
    case AUTHENTICATION_ERROR:
      return state.set('error', action.payload);
    default:
      return state
  }
}