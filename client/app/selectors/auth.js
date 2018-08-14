import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const auth = state => state.get('global').get('auth')

export const isAuthenticated = createSelector(
  auth,
  state => state.get('isAuthenticated')
)
