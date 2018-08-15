import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const getLobbyState = state => state.get('global').get('lobby')

export const getLobbyTables = createSelector(
    getLobbyState,
    state => state.get('tables')
)

