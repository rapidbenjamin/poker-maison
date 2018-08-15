import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const getLobbyState = state => state.get('global').get('lobby')
