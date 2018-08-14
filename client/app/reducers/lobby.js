import Immutable from 'immutable';

import * as types from '../actions/types';

const initialState = Immutable.Map({ lobby: null });

export default function (state = initialState, action) {
  switch (action.type) {
    case types.NEW_LOBBY:
      return state.set('lobby', action.lobby);
    default:
      return state
  }
}