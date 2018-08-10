import {
  combineReducers
} from 'redux-immutable';

import authReducer from './auth';

const rootReducer = combineReducers({
  auth: authReducer
});

export default rootReducer;
