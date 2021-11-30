def fib(n)
    arr = Array.new

    if n >= 1 then
        arr[0] = 0
    end

    if n >= 2 then
        arr[1] = 1
    end

    i = 2
    while i < n do
        arr[i] = arr[i-2] + arr[i-1]
        i = i + 1
    end
    
    arr
end

def isPalindrome(n)
    num = n.to_s
    rev = num.reverse

    if num == rev then
        true
    else
        false
    end
end

def nthmax(n, a)
    if n > a.length - 1 then
        return nil
    end

    sorted = a.sort
    sorted = sorted.reverse

    sorted[n]   
end

def freq(s)
    if s.length == 0 then
        ""
    else
        max = -1000
        result = ""

        for i in 0..s.length-1
            num = s.count(s[i])

            if num > max then
                result = s[i]
                max = num
            end
        end

        result
    end
end

def zipHash(arr1, arr2)
    if arr1.length != arr2.length then
        nil
    else
        hash = Hash.new

        for i in 0..arr2.length-1
            hash[arr1[i]] = arr2[i]
        end

        hash
    end
end

def hashToArray(hash)
    arr = Array.new

    if hash.length == 0 then
        return arr
    end

    keys = hash.keys
    vals = hash.values

    for i in 0..hash.length-1
        arr[i] = Array.new
        arr[i][0] = keys[i].dup
        arr[i][1] = vals[i].dup
    end

    arr
end
