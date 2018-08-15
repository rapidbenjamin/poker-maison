import Immutable from 'immutable';

import * as types from '../actions/types';

const initialState = Immutable.Map({});

export default function (state = initialState, action) {
  switch (action.type) {
    case types.NEW_GAME_STATE:
      return state.set(action.tableName, action.gameState);
    default:
      return state
  }
}