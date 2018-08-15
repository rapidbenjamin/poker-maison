import Immutable, { fromJS } from 'immutable';

import * as types from '../actions/types';

const initialState = Immutable.fromJS([]);

export default function (state = initialState, action) {
  switch (action.type) {
    case types.NEW_LOBBY:
      return fromJS(action.lobby)
    default:
      return state
  }
}