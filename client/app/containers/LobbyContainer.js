import React from 'react'
import { connect } from "react-redux";
import { withRouter } from 'react-router-dom'

import { getLobby, takeSeat } from '../actions/lobby'
import { getLobbyState } from '../selectors/lobby'
import Lobby from '../components/Lobby'

class LobbyContainer extends React.Component {
  componentDidMount() {
    this.props.getLobby()
  }

  render() {
    const { lobby, takeSeat } = this.props
    return (<Lobby lobby={lobby} takeSeat={takeSeat} />)
  }
}

const mapStateToProps = state => ({
  lobby: getLobbyState(state)
});

const mapDispatchToProps = (dispatch, { history }) => ({
  getLobby: () => dispatch(getLobby()),
  takeSeat: (tableName, chips) => dispatch(takeSeat(tableName, chips, history))
});

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(LobbyContainer));
