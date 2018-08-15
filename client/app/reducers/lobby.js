import Immutable from 'immutable';

import * as types from '../actions/types';

const initialState = Immutable.fromJS({ tables: [] });

export default function (state = initialState, action) {
  switch (action.type) {
    case types.NEW_LOBBY:
      return state.set('tables', action.lobby)
    default:
      return state
  }
}