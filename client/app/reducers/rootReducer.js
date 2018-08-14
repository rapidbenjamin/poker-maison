import {
  combineReducers
} from 'redux-immutable';

import auth from './auth';
import socket from './socket'
import lobby from './lobby'

const rootReducer = combineReducers({
  auth,
  socket,
  lobby
});

export default rootReducer;
