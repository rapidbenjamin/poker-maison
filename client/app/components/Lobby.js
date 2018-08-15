import React from 'react';

const tableBody = (lobby, takeSeat) =>
  <tbody >
    {lobby.map((table) => {
      const tableName = table.get('_tableName')

      return <tr key={tableName} onClick={() => takeSeat(tableName, 2000)}>
        <td>{tableName}</td>
        <td>{`${table.get('_playerCount')} / ${table.get('_maxPlayers')}`}</td>
        <td>{table.get('_waitlistCount')}</td>
        <td>{table.get('_minBuyInChips')}</td>
        <td>{table.get('_maxBuyInChips')}</td>
        <td>{table.get('_bigBlind')}</td>
      </tr>
    })}
  </tbody >

const Lobby = ({ lobby, takeSeat }) =>
  <table className="table">
    <thead>
      <tr>
        <th>Name</th>
        <th>Players</th>
        <th>Waitlist</th>
        <th>Min BuyIn</th>
        <th>Max BuyIn</th>
        <th>Big Blind</th>
      </tr>
    </thead>
    {tableBody(lobby, takeSeat)}
  </table>

export default Lobby;
