require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    board = GameBoard.new 10, 10

    ships = Array.new

    success = read_file_lines(path) { |line|   
        valid = true
        fields = line.split(", ")

        position = fields[0].match(/^\((?<row>\d+),(?<col>\d+)\)$/)

        if !position then
            valid = false
        else
            row = position["row"].to_i
            col = position["col"].to_i

            if row < 1 || row > 10 || col < 1 || col > 10 then
                valid = false
            end
        end

        if fields[1].match(/^[A-Z][a-z]+$/) then
            orientation = fields[1]
            
            if orientation != "Up" && orientation != "Down" && orientation != "Left" && orientation != "Right" then
                valid = false;
            end
        else
            valid = false
        end

        if fields[2].match(/^\d$/) then
            size = fields[2].to_i

            if size < 1 || size > 5 then
                valid = false;
            end
        else
            valid = false
        end

        if valid then
            ships.push(Ship.new(Position.new(row, col), orientation, size))
        end
    }

    if !success then
        return nil
    end

    if ships.length < 5 then
        return nil
    else
        for i in 0..4
            board.add_ship(ships[i])
        end

        return board
    end
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    positions = Array.new

    success = read_file_lines(path) { |line|
    
        position = line.match(/^\((?<row>\d+),(?<col>\d+)\)$/)

        if position then
            row = position["row"].to_i
            col = position["col"].to_i
            positions.push(Position.new(row, col))
        end
          
    }

    if !success then
        return nil
    else
        return positions
    end
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
