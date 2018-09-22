import Immutable from 'immutable'
import { createSelector } from 'reselect'

export const getLocation = state => state.get('route').get('location')

export const getPathname = createSelector(
  getLocation,
  location => (location ? location.get('pathname') : null)
)
