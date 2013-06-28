########################################################################################################################
## quick sort implementation
##
## Practice for Google's interviews
##
## Francisco Soto <ebobby@ebobby.org>
########################################################################################################################

class QuickSort
  class << self
    private
    def partition (arr, left, right)
      pivot = left + ((right - left) >> 1)
      pivot_value = arr[pivot]
      store_index = left
      arr[pivot], arr[right] = arr[right], arr[pivot]

      for i in left..(right-1)
        if arr[i] < pivot_value
          arr[i], arr[store_index] = arr[store_index], arr[i] unless i == store_index
          store_index += 1
        end
      end

      arr[store_index], arr[right] = arr[right], arr[store_index]
      store_index
    end

    public
    def sort (arr, left = 0, right = arr.size - 1)
      if left < right
        index = partition(arr, left, right)
        sort(arr, left, index - 1)
        sort(arr, index + 1, right)
      end
      arr
    end
  end
end
