
mEditor = reaper.MIDIEditor_GetActive()
if mEditor then
	mTake = reaper.MIDIEditor_GetTake(mEditor)
	if mTake then
		local _retval, num_notes, num_cc, num_sysex = reaper.MIDI_CountEvts(mTake)
		
		if num_notes > 0 then 
			t = {};
			
			for i = 1, num_notes do
				_retval, selected, muted, startppq, endppq, channel, pitch, velocity = reaper.MIDI_GetNote(mTake, i-1)
				t[i] = {}
				t[i][1] = selected
				t[i][2] = muted
				t[i][3] = math.floor(startppq)
				t[i][4] = math.floor(endppq)
				t[i][5] = math.floor(endppq-startppq)
				t[i][6] = channel
				t[i][7] = pitch
				t[i][8] = velocity
			end     
			
		end
		
		i = 1
		str = "sel \tmut \ts_ppq \te_ppq \tleng \tchan \tpitch \tvel \n"
			
		while t[i] do
			j = 1
			
			while (t[i][j] ~= nil)   do     
				str = str .. tostring(t[i][j]) .. "\t"
				j = j + 1
			end     
			
		str = str .. "\n"
		i = i + 1
		end -- while t[i]
		
		str = str .. "\n"
		reaper.ShowConsoleMsg(str .."\n")
	end -- no take
end

--tempfile = io.tempfile
--opening the file in write mode to write the contents to the file using write() operation and then closing the file
--filewrite = io.open("tempfile", "w")
--filewrite:write(t)
--filewrite:close()
print(t)

