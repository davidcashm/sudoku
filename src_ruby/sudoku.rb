#!/usr/bin/env ruby

require 'set'

Chars = "123456789"

def idx_at(x,y)
  return x+9*y
end

def xy_at(idx)
  return [idx%9, idx/9]
end
  
def compute_rows
  rows = Array.new
  81.times do |idx|
    base = idx - (idx % 9)
    rows.push (base..base+8).to_set.delete(idx)
  end
  rows
end
$rows = compute_rows

def compute_columns
  columns = Array.new
  81.times do |idx|
    base = idx % 9
    columns.push (base..80).step(9).to_set.delete(idx)
  end
  columns
end
$columns = compute_columns

def compute_boxes
  boxes = Array.new
  81.times do |idx|
    x,y = xy_at(idx)
    xbase = x - (x%3)
    ybase = y - (y%3)
    box = Set.new []
    (xbase..xbase+2).each do |x1|
      (ybase..ybase+2).each do |y1|
        box.add(idx_at(x1,y1))
      end
    end
    box.delete idx
    boxes.push box
  end
  boxes
end
$boxes = compute_boxes
    
def compute_peers
  peers = Array.new
  81.times do |idx|
    peers.push ($rows[idx] | $columns[idx] | $boxes[idx])
  end
  peers
end
  
$peers = compute_peers

class Board
  def initialize()
    @array = Array.new
    81.times do
      @array.push Chars.dup
    end
  end
  
  def clone
    b = Board.new
    (0..80).each do |idx|
      # Hmm, do I really need instance_variable_get here?
      b.instance_variable_get(:@array)[idx] = @array[idx].dup
    end
    return b
  end

  def assign (val, idx)
    result = true
    @array[idx].delete(val).split("").each do |c|
      result = result && eliminate(c, idx)
    end
    result
  end
  
  def at(idx)
    @array[idx]
  end

  def all_solved
    unsolved = @array.select {|v| v.length != 1}
    return unsolved.length == 0
  end

  def illegal
    illegals = @array.select {|v| v.length == 0}
    return illegals.length > 0
  end

  # Remove val from the string at idx
  def eliminate (val, idx)
    # Note that delete! does in-place deletion, and 
    # returns nil if the character isn't found
    if @array[idx].delete!(val)
      new_length = @array[idx].length
      if new_length == 0
        return false
      elsif new_length == 1
        val2 = @array[idx]
        result = true
        $peers[idx].each do |idx2|
          result = result && eliminate(val2, idx2)
        end
        if !result
          return false
        end
      end

      # Check if deleting this value means that there's only
      # one of this value left in the current row/column/box
      result = true
      [$rows[idx], $columns[idx], $boxes[idx]].each do |unit|
        places = unit.select { |idx| @array[idx].count(val) > 0 }
        if places.length == 0
          return false
        elsif places.length == 1
          # val must go at that location
          result = result && assign(val, places[0])
        end
      end
      if !result
        return false
      end
    end
    true
  end

  def print_board(with_unknowns)
    (0..2).each do |y_sector|
      if y_sector > 0
        puts "------+-------+-------"
      end
      (0..2).each do |y_row|
        y = 3*y_sector + y_row
        rowstring = ""
        @array[9*y .. 9*y+8].each_with_index do |c, i|
          if !with_unknowns && c.length != 1 
            rowstring += "."
          else 
            rowstring += c
          end
          rowstring += " "
          if i == 2 || i == 5 
            rowstring += "|"
            rowstring += " "
          end
        end
        puts rowstring
      end
    end
    puts ""
  end
end

def solve_sudoku(line)
  board = Board.new
  solve(board,line)
end
  
def solve(b, line)
  result = true
  line.strip.split("").each_with_index do |c, i|
    if c != "."
      result = result && b.assign(c,i)
    end
  end

  b = search(b)
  
  if b
    b.print_board(true)
  end
  result
end

def search(b)
  if b.all_solved
    return b
  end
  
  if b.illegal
    return nil
  end

#   b.print_board(true)

  pairs = (0..80).map {|idx| [b.at(idx).split("").length,idx]}
  pairs = pairs.select { |p| p[0] > 1 }
  pairs.sort! { |a, b| a[0] <=> b[0] }

  idx = pairs[0][1]
  vals = b.at(idx)
  vals.split("").each do |val|
    b2 = b.clone
    b2.assign(val, idx)
    result = search(b2)
    if result
      return result
    end
  end

  nil
end

if __FILE__ == $0
  fname = "../top95.txt"
  File.open(fname) do |fh|
    fh.each_line do |line|
      solution = solve_sudoku line
    end
  end
end
