import React from 'react'
import { connect } from "react-redux";

import Game from '../components/Game'
import { getGame, getPlayerPosition, isTurnToAct } from '../selectors/games'

class GameContainer extends React.Component {
  render() {
    const { game, isTurnToAct, username } = this.props
    console.log(this.props)
    return (<Game game={game} username={username} isTurnToAct={isTurnToAct} />)
  }
}

const mapStateToProps = (state, { username, match: { params: { tableName } } }) => ({
  game: getGame(tableName)(state),
  isTurnToAct: isTurnToAct(username, tableName)(state),
  playerPosition: getPlayerPosition(tableName)(state)
});

const mapDispatchToProps = (dispatch, { match: { params: { tableName } } }) => ({
  // game: () => dispatch(getLobby()),
});


export default connect(mapStateToProps, mapDispatchToProps)(GameContainer)
