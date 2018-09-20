import { combineReducers } from 'redux-immutable'

import auth from './auth'
import socket from './socket'
import lobby from './lobby'
import games from './games'
import profile from './profile'

const rootReducer = combineReducers({
  auth,
  socket,
  lobby,
  profile,
  games
})

export default rootReducer
