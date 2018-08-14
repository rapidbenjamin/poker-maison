import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const isAuthenticated = state => state.get('auth').get('isAuthenticated')
