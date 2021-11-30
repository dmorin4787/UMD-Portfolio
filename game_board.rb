class GameBoard
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @hits = 0
        @ships = 0
        @grid = Array.new(max_row) {Array.new(max_column)} 

        for i in 0..@max_row - 1
            for j in 0..@max_column - 1
                @grid[i][j] = Hash.new
                @grid[i][j]["ship"] = "E"
                @grid[i][j]["attack"] = "N"
            end
        end

    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        
        row = ship.start_position.row
        col = ship.start_position.column
        success = false

        if row < 1 or row > @max_row or col < 1 or col > @max_column then
            return false
        end

        if ship.orientation == "Right" and (col + ship.size - 1) <= @max_column then
            for i in col..(col + ship.size - 1)
                if @grid[row - 1][i - 1]["ship"] == "S" then
                    return false
                else
                    @grid[row - 1][i - 1]["ship"] = "S"
                end
            end

            success = true
        elsif ship.orientation == "Left" and (col - (ship.size - 1)) >= 1 then
            for i in (col - (ship.size - 1))..col
                if @grid[row - 1][i - 1]["ship"] == "S" then
                    return false
                else
                    @grid[row - 1][i - 1]["ship"] = "S"
                end
            end

            success = true
        elsif ship.orientation == "Up" and (row - (ship.size - 1)) >= 1 then
            for i in (row - (ship.size - 1))..row
                if @grid[i - 1][col - 1]["ship"] == "S" then
                    return false
                else
                    @grid[i - 1][col - 1]["ship"] = "S"
                end
            end

            success = true
        elsif ship.orientation == "Down" and (row + ship.size - 1) <= @max_row then
            for i in row..(row + ship.size - 1)
                if @grid[i - 1][col - 1]["ship"] == "S" then
                    return false
                else
                    @grid[i - 1][col - 1]["ship"] = "S"
                end
            end

            success = true
        end

        if success then
            @ships += 1
            return true
        else
            return false
        end
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)

        row = position.row
        col = position.column
        success = false

        # check position
        if row < 1 or row > @max_row or col < 1 or col > @max_column then
            return nil
        end

        # update your grid
        if @grid[row - 1][col - 1]["ship"] == "S" then
            success = true

            if @grid[row - 1][col - 1]["attack"] == "N" then
                @hits += 1
            end
        end

        @grid[row - 1][col - 1]["attack"] = "A"
        
        # return whether the attack was successful or not
        return success
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @hits
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        if @ships == 0 then 
            return false
        end

        for i in 0..(@max_row - 1)
            for j in 0..(@max_column - 1)
                if @grid[i][j]["ship"] == "S" and @grid[i][j]["attack"] == "N" then
                    return false
                end
            end
        end

        return true
    end

    # String representation of GameBoard (optional but recommended)
    def to_s
        "STRING METHOD IS NOT IMPLEMENTED"
    end
end
