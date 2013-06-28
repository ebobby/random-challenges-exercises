########################################################################################################################
## merge sort implementation
##
## Practice for Google's interviews
##
## Francisco Soto <ebobby@ebobby.org>
########################################################################################################################

class MergeSort
  class << self
    private
    def merge (ar1, ar2)
      result = Array.new( ar1.size + ar2.size )
      t = 0
      i = 0
      j = 0
      while i + j != ar1.size + ar2.size
        if i < ar1.size and j < ar2.size
          if ar1[i] < ar2[j]
            val = ar1[i]
            i += 1
          else
            val = ar2[j]
            j += 1
          end
          result[t] = val
          t += 1
        elsif i < ar1.size
          result[t] = ar1[i]
          i += 1
          t += 1
        elsif j < ar2.size
          result[t] = ar2[j]
          j += 1
          t += 1
        end
      end
      result
    end

    public
    def sort (ar)
      return ar if ar.size == 1
      middle = ar.size >> 1
      merge( sort(ar[0, middle]), sort(ar[middle, ar.size]))
    end
  end
end
