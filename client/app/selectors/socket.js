import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const socket = state => state.get('global').get('socket')

export const isSocketAuthenticated = createSelector(
    socket,
    state => state.get('isSocketAuth')
)
