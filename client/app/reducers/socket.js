
import Immutable, { fromJS } from 'immutable';

import {
  SOCKET_AUTH_SUCCESS,
  SOCKET_AUTH_ERR,
  SOCKET_CONN_ERR,
  SOCKET_CONNECTED,
  DISCONNECT_SOCKET
} from '../actions/types'

const initialState = fromJS({
  socketAuth: false,
  socketAuthErr: null,
  socketConnErr: null,
  socketConnected: false
})

export default function socket(state = initialState, action) {
  switch (action.type) {
    case SOCKET_CONNECTED:
      return state.set('socketConnected', true).set('socketConnErr', null).set('socketAuthErr', null)
    case DISCONNECT_SOCKET:
      return state.set('socketConnected', false).set('socketConnErr', null).set('socketAuthErr', null)
    case SOCKET_AUTH_SUCCESS:
      return state.set('socketAuth', true)
    case SOCKET_AUTH_ERR:
      return state.set('socketAuthError', action.err)
    case SOCKET_CONN_ERR:
      return state.set('socketConnError', action.err)
    default:
      return state
  }
}