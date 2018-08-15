import {
  combineReducers
} from 'redux-immutable';

import auth from './auth';
import socket from './socket'
import lobby from './lobby'
import games from './games'

const rootReducer = combineReducers({
  auth,
  socket,
  lobby,
  games
});

export default rootReducer;
