import React from 'react'
import { connect } from "react-redux";

import Game from '../components/Game'
import { getGame, getPlayerPosition, isTurnToAct } from '../selectors/games'
import { call, bet, fold, raise, check, postBigBlind, postSmallBlind, sitIn, leaveSeat } from '../actions/games'
import { takeSeat } from '../actions/lobby'

class GameContainer extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      betValue: 0
    }
  }
  handleChange = event => {
    this.setState({ betValue: event.target.value });
  }

  render() {
    return (<Game updateBetValue={this.handleChange} betValue={this.state.betValue} {...this.props} />)
  }
}

const mapStateToProps = (state, { username, match: { params: { tableName } } }) => ({
  game: getGame(tableName)(state),
  isTurnToAct: isTurnToAct(username, tableName)(state),
  playerPosition: getPlayerPosition(tableName)(state)
});

const mapDispatchToProps = (dispatch, { match: { params: { tableName } } }) => ({
  bet: amount => dispatch(bet(tableName, amount)),
  raise: amount => dispatch(raise(tableName, amount)),
  call: () => dispatch(call(tableName)),
  fold: () => dispatch(fold(tableName)),
  check: () => dispatch(check(tableName)),
  postSmallBlind: () => dispatch(postSmallBlind(tableName)),
  postBigBlind: () => dispatch(postBigBlind(tableName)),
  sitDown: chips => dispatch(takeSeat(tableName, chips)),
  sitIn: () => dispatch(sitIn(tableName)),
  leaveGameSeat: () => dispatch(leaveSeat(tableName))
});


export default connect(mapStateToProps, mapDispatchToProps)(GameContainer)
