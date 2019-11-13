import React from 'react';

const Lobby = ({ lobby, history, subscribeToATable }) =>
  < table className="table game-table-list" >
    <thead>
      <tr>
        <th><h4>Name</h4></th>
        <th><h4>Players</h4></th>
        <th><h4>Waitlist</h4></th>
        <th><h4>Min Buy In</h4></th>
        <th><h4>Max Buy In</h4></th>
        <th><h4>Big Blind</h4></th>
        <th></th>
      </tr>
    </thead>
    <tbody >
      {lobby.map((table) => {
        const tableName = table.get('_tableName')

        return <tr
          key={tableName}
        >
          <td>{tableName}</td>
          <td>{`${table.get('_playerCount')} / ${table.get('_maxPlayers')}`}</td>
          <td>{table.get('_waitlistCount')}</td>
          <td>{table.get('_minBuyInChips')}</td>
          <td>{table.get('_maxBuyInChips')}</td>
          <td>{table.get('_bigBlind')}</td>
          <td><button className="button" style={{ fontSize: "0.8em", paddingBottom: '0.2em', paddingTop: '0.2em', width: "5.7em", height: "3.8em" }} onClick={() => {
            subscribeToATable(tableName)
            history.push(`/game/${tableName}`)
          }}>Join</button></td>
        </tr>
      })}
    </tbody >
  </table >

export default Lobby;
