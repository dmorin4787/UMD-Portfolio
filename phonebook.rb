class PhoneBook
    def initialize
        @phonebook = Array.new
    end

    def add(name, number, is_listed)
        length = @phonebook.length
        
        for i in 0..length-1
            if @phonebook[i][0] == name then
                return false
            end
        end

        matchForm = number.match /^\d{3}-\d{3}-\d{4}$/

        if matchForm == nil then
            return false
        end

        for i in 0..length-1
            if @phonebook[i][1] == number then
                if @phonebook[i][2] == true and is_listed == true then
                    return false
                end
            end
        end

        @phonebook[length] = Array.new
        @phonebook[length][0] = name
        @phonebook[length][1] = number
        @phonebook[length][2] = is_listed

        true
    end

    def lookup(name)
        
        for i in 0..@phonebook.length-1
            if @phonebook[i][0] == name and @phonebook[i][2] == true then
                return @phonebook[i][1]
            end
        end

        nil
    end

    def lookupByNum(number)
        
        for i in 0..@phonebook.length-1
            if @phonebook[i][1] == number and @phonebook[i][2] == true then
                return @phonebook[i][0]
            end
        end

        nil
    end

    def namesByAc(areacode)
        namesArr = Array.new

        for i in 0..@phonebook.length-1
            if @phonebook [i][1][0,3] == areacode then
                namesArr.push(@phonebook[i][0])
            end
        end

        namesArr
    end
end
