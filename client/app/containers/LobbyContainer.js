import React from 'react'
import { connect } from "react-redux";

import { getLobby, takeSeat } from '../actions/lobby'
import { getLobbyTables } from '../selectors/lobby'
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
  lobby: getLobbyTables(state)
});

const mapDispatchToProps = dispatch => ({
  getLobby: () => dispatch(getLobby()),
  takeSeat: (tableName, chips) => dispatch(takeSeat(tableName, chips))
});

export default connect(mapStateToProps, mapDispatchToProps)(LobbyContainer);
