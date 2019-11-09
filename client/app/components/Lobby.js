import React from 'react';

const Lobby = ({ lobby, history, subscribeToATable }) =>
  < table className="table game-table-list" >
    <thead>
      <tr>
        <th>Name</th>
        <th>Players</th>
        <th>Waitlist</th>
        <th>Min Buy In</th>
        <th>Max Buy In</th>
        <th>Big Blind</th>
      </tr>
    </thead>
    <tbody >
      {lobby.map((table) => {
        const tableName = table.get('_tableName')

        return <tr
          key={tableName}
          onClick={() => {
            subscribeToATable(tableName)
            history.push(`/game/${tableName}`)
          }}>
          <td>{tableName}</td>
          <td>{`${table.get('_playerCount')} / ${table.get('_maxPlayers')}`}</td>
          <td>{table.get('_waitlistCount')}</td>
          <td>{table.get('_minBuyInChips')}</td>
          <td>{table.get('_maxBuyInChips')}</td>
          <td>{table.get('_bigBlind')}</td>
        </tr>
      })}
    </tbody >
  </table >

export default Lobby;
