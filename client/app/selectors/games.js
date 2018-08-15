import Immutable from 'immutable';
import { createSelector } from 'reselect'

export const getGames = state => state.get('global').get('games')

export const getGame = () => null