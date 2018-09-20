import Immutable from 'immutable'
import { createSelector } from 'reselect'

export const profile = state => state.get('global').get('profile')

export const getProfile = createSelector(profile, state => state.get('profile'))
