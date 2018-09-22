import Immutable from 'immutable'
import { createSelector } from 'reselect'

export const profile = state => ({})

export const getProfileSelector = createSelector(profile, profile => ({}))
